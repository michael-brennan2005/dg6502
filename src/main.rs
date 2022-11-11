use std::{fs::File, io::BufReader};
use std::io::Read;
use bus::Bus;
use cpu::Cpu;
use eframe::egui;
use egui::{Window, TextEdit};
pub mod bus;
pub mod cpu;

pub struct Ui<T: Bus> {
    cpu: Cpu<T>,
    start_address: String,
    memory_rows: String
}

impl<T: Bus> Ui<T> {
    fn new(cpu: Cpu<T>) -> Self {
        Ui {
            cpu,
            start_address: String::from("0"),
            memory_rows: String::from("32")
        }
    }
}
impl<T: Bus> eframe::App for Ui<T> {
    fn update(&mut self, ctx: &egui::Context, _: &mut eframe::Frame) {
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
                        self.cpu.bus.read(i * 16 + 0),
                        self.cpu.bus.read(i * 16 + 1),
                        self.cpu.bus.read(i * 16 + 2),
                        self.cpu.bus.read(i * 16 + 3),
                        self.cpu.bus.read(i * 16 + 4),
                        self.cpu.bus.read(i * 16 + 5),
                        self.cpu.bus.read(i * 16 + 6),
                        self.cpu.bus.read(i * 16 + 7),
                        self.cpu.bus.read(i * 16 + 8),
                        self.cpu.bus.read(i * 16 + 9),
                        self.cpu.bus.read(i * 16 + 10),
                        self.cpu.bus.read(i * 16 + 11),
                        self.cpu.bus.read(i * 16 + 12),
                        self.cpu.bus.read(i * 16 + 13),
                        self.cpu.bus.read(i * 16 + 14),
                        self.cpu.bus.read(i * 16 + 15),).as_str();
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
                        ui.label(format!("{}", self.cpu.program_counter));
                        ui.label(format!("0x{:04X}", self.cpu.program_counter));
                        ui.label(format!("0b{:016b}", self.cpu.program_counter));
                        
                        ui.end_row();
                        
                        ui.label("ACC");
                        ui.label(format!("{}", self.cpu.accumulator));
                        ui.label(format!("0x{:02X}", self.cpu.accumulator));
                        ui.label(format!("0b{:08b}", self.cpu.accumulator));
                        ui.end_row();
                        
                        ui.label("X");
                        ui.label(format!("{}", self.cpu.x));
                        ui.label(format!("0x{:02X}", self.cpu.x));
                        ui.label(format!("0b{:08b}", self.cpu.x));
                        ui.end_row();
                        
                        ui.label("Y");
                        ui.label(format!("{}", self.cpu.y));
                        ui.label(format!("0x{:02X}", self.cpu.y));
                        ui.label(format!("0b{:08b}", self.cpu.y));
                        ui.end_row();
                        
                        ui.label("SP");
                        ui.label(format!("{}", self.cpu.stack_pointer));
                        ui.label(format!("0x{:02X}", self.cpu.stack_pointer));
                        ui.label(format!("0b{:08b}", self.cpu.stack_pointer));
                        ui.end_row();

                    });

            });

        Window::new("Stepper")
            .resizable(true)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label(format!("PC: {:04X}", self.cpu.program_counter));
                    if ui.button("Step").clicked() {
                        self.cpu.step();
                    }
                });
            });
    }

    fn clear_color(&self, _visuals: &egui::Visuals) -> egui::Rgba {
        egui::Rgba::from_rgb(0.1, 0.1, 0.1)
    }
}

fn main() {
    let f = File::open("example.bin").unwrap();
    let mut reader = BufReader::new(f);
    let mut buffer: Vec<u8> = Vec::new();

    reader.read_to_end(&mut buffer).unwrap();

    let bus = bus::BasicBus::try_from(buffer).unwrap();
    let cpu6502 = cpu::Cpu::new(bus);

    eframe::run_native("My egui app", eframe::NativeOptions::default(), Box::new(|_cc| Box::new(Ui::new(cpu6502))));
}
