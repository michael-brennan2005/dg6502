use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::thread;
use std::{fs::File, io::BufReader};
use std::io::Read;
use bus::{Bus, BasicBus};
use cpu::Cpu;
use eframe::egui;
use egui::{Window, TextEdit};
pub mod bus;
pub mod cpu;

pub struct Ui {
    cpu: Arc<Mutex<Cpu<BasicBus>>>,
    running: Arc<Mutex<bool>>,
    start_address: String,
    memory_rows: String,
    start_address_load: String
}

impl Ui {
    fn new(cpu: Arc<Mutex<Cpu<BasicBus>>>, running: Arc<Mutex<bool>>) -> Self {
        Ui {
            cpu,
            start_address: String::from("0"),
            memory_rows: String::from("32"),
            start_address_load: String::from("0x0"),
            running
        }
    }
}

impl eframe::App for Ui {
    fn update(&mut self, ctx: &egui::Context, _: &mut eframe::Frame) {
        let mut cpu = self.cpu.lock().unwrap();
        let mut running = self.running.lock().unwrap();
        // MEMORY VIEW

        let mut memory_view = String::new();
        memory_view += "       00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n";
        let mut memory_rows = self.memory_rows.parse::<u16>().unwrap_or(32);
        let mut start_address = u16::from_str_radix(self.start_address.as_str(), 16).unwrap_or(16);
        
        if start_address + memory_rows > 0xFFF {
            start_address = 0;
            memory_rows = 32;
        }

        for i in (start_address)..(start_address + memory_rows) {
            
            memory_view += 
                format!("0x{:04X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X}\n", 
                        i * 16,
                        cpu.bus.read(i * 16 + 0),
                        cpu.bus.read(i * 16 + 1),
                        cpu.bus.read(i * 16 + 2),
                        cpu.bus.read(i * 16 + 3),
                        cpu.bus.read(i * 16 + 4),
                        cpu.bus.read(i * 16 + 5),
                        cpu.bus.read(i * 16 + 6),
                        cpu.bus.read(i * 16 + 7),
                        cpu.bus.read(i * 16 + 8),
                        cpu.bus.read(i * 16 + 9),
                        cpu.bus.read(i * 16 + 10),
                        cpu.bus.read(i * 16 + 11),
                        cpu.bus.read(i * 16 + 12),
                        cpu.bus.read(i * 16 + 13),
                        cpu.bus.read(i * 16 + 14),
                        cpu.bus.read(i * 16 + 15),).as_str();
        }
        Window::new("Memory view")
            .vscroll(true)
            .resizable(true)
            .default_height(300.0)
            .default_width(600.0)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label("Start address:");
                    ui.text_edit_singleline(&mut self.start_address);
                    ui.label("# of rows:");
                    ui.text_edit_singleline(&mut self.memory_rows);
                });
                ui.add_sized(ui.available_size(), TextEdit::multiline(&mut memory_view).font(egui::TextStyle::Monospace));
            });

        // REGISTER VIEW
        Window::new("Register view")
            .resizable(true)
            .show(ctx, |ui| {
                egui::Grid::new("register_grid")
                    .num_columns(4)
                    .spacing([40.0,4.0])
                    .striped(true)
                    .show(ui, |ui| {
                        ui.label("reg");
                        ui.label("dec");
                        ui.label("hex");
                        ui.label("bin");
                        ui.end_row();

                        ui.label("PC");
                        ui.label(format!("{}", cpu.program_counter));
                        ui.label(format!("0x{:04X}", cpu.program_counter));
                        ui.label(format!("0b{:016b}", cpu.program_counter));
                        
                        ui.end_row();
                        
                        ui.label("ACC");
                        ui.label(format!("{}", cpu.accumulator));
                        ui.label(format!("0x{:02X}", cpu.accumulator));
                        ui.label(format!("0b{:08b}", cpu.accumulator));
                        ui.end_row();
                        
                        ui.label("X");
                        ui.label(format!("{}", cpu.x));
                        ui.label(format!("0x{:02X}", cpu.x));
                        ui.label(format!("0b{:08b}", cpu.x));
                        ui.end_row();
                        
                        ui.label("Y");
                        ui.label(format!("{}", cpu.y));
                        ui.label(format!("0x{:02X}", cpu.y));
                        ui.label(format!("0b{:08b}", cpu.y));
                        ui.end_row();
                        
                        ui.label("SP");
                        ui.label(format!("{}", cpu.stack_pointer));
                        ui.label(format!("0x{:02X}", cpu.stack_pointer));
                        ui.label(format!("0b{:08b}", cpu.stack_pointer));
                        ui.end_row();

                    });

            });
        
        // STEPPER 
        Window::new("Stepper")
            .resizable(true)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label(format!("PC: {:04X}", cpu.program_counter));
                    if ui.button("Step").clicked() {
                       cpu.step();
                    }
                });
                ui.horizontal(|ui| {
                    ui.label(format!("{}", if *running { "RUNNING..."} else { "STOPPED. "}));
                    if ui.button("Start").clicked() {
                        *running = true;
                    }
                    if ui.button("Stop").clicked() {
                        *running = false;
                    }
                })
            });

        // LOADER
        Window::new("Loader")
            .resizable(true)
            .show(ctx, |ui| {
                ui.vertical(|ui| {
                    if ui.button("Load file...").clicked() {
                        if let Some(path) = rfd::FileDialog::new().pick_file() {
                            let f = File::open(path).unwrap();
                            let mut reader = BufReader::new(f);
                            let mut buffer: Vec<u8> = Vec::new();

                            reader.read_to_end(&mut buffer).unwrap();

                            cpu.bus.swap_buffer(buffer);
                        }
                    }
                    ui.label("Start address");
                    ui.text_edit_singleline(&mut self.start_address_load);
                })
            });
    }

    fn clear_color(&self, _visuals: &egui::Visuals) -> egui::Rgba {
        egui::Rgba::from_rgb(0.1, 0.1, 0.1)
    }
}

fn main() {
    let buffer: Vec<u8> = Vec::new();
    let bus = bus::BasicBus::try_from(buffer).unwrap();

    let cpu6502 = Arc::new(Mutex::new(cpu::Cpu::new(bus)));
    let running = Arc::new(Mutex::new(false));

    let cpu_run_thread = cpu6502.clone();
    let running_run_thread = running.clone();

    thread::spawn(move || {
        loop {
            let running = running_run_thread.lock().unwrap();
            if *running {
                let cpu = cpu_run_thread.lock().unwrap();
                println!("{}", cpu.accumulator);
                drop(cpu);
            }
            drop(running);
            thread::sleep(Duration::from_millis(100));    
        }
    });

    let cpu_ui_thread = cpu6502.clone();
    let running_ui_thread = running.clone();
    eframe::run_native("My egui app", eframe::NativeOptions::default(), Box::new(|_cc| Box::new(Ui::new(cpu_ui_thread, running_ui_thread))));
}
