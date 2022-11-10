use std::{fs::File, io::BufReader};
use std::io::Read;
use eframe::egui;
use egui::{Window, TextEdit};
pub mod bus;
pub mod cpu;

pub struct Ui();

impl eframe::App for Ui {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        let mut memory_view = String::new();
        memory_view += "       00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n";
        for i in 0..64 {
            memory_view += format!("0x{:04X}\n", i * 16).as_str();
        }
        Window::new("Memory view")
            .vscroll(true)
            .resizable(true)
            .default_height(300.0)
            .show(ctx, |ui| {
                ui.add_sized(ui.available_size(), TextEdit::multiline(&mut memory_view).font(egui::TextStyle::Monospace));
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
    let mut cpu6502 = cpu::Cpu::new(bus);

    eframe::run_native("My egui app", eframe::NativeOptions::default(), Box::new(|_cc| Box::new(Ui{})));
}
